#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <stddef.h>
 
#define AF_INET 1024
typedef char in_addr_t;

void validate_addr_form(char *user_supplied_addr){
    return ;
}

in_addr_t inet_addr(const char *cp);

typedef struct hostent {
  char  h_name[256];
} HOSTENT, *PHOSTENT, *LPHOSTENT;

struct hostent *gethostbyaddr(char *host_address,
                              int address_length,
                              int address_type);

/*@   strcpy(des, src)  :  strcpy(des, src)  @*/
/*@   strlen(str)       :  strlength(str, ret)  @*/
/*@   IfStmt(a > b)     :  checkGT(a, b) @*/ // , checkGTEQ(a, b)
     
/* 
     NotBuffer_Overflow(des, src, l) :- strcpy(des, src, l), 
                                   transFlow(l1, l), check(des, src, l1), 
                                   transFlow(l2, l1), Entry (l2).  

     Buffer_Overflow (des, src, l) :- strcpy(des, src, l), ! NotBuffer_Overflow(des, src, l).
@*/


void host_lookup(char *user_supplied_addr){
    struct hostent *hp;
    in_addr_t *addr;
    char hostname[64];

/*routine that ensures user_supplied_addr is in the right format for conversion */ 

    validate_addr_form(user_supplied_addr);
    *addr = inet_addr(user_supplied_addr);
    hp = gethostbyaddr(addr, 4, AF_INET);
    int a = strlen(hostname) ; 
    int b = strlen(hp->h_name) ;
    //if (a > b){
        strcpy(hostname, hp->h_name);
        printf ("%s", hostname);   
    //}
    return; 
    
}

int main()
{
    char * address = "address"; 
    host_lookup (address);
    return 0;
}

// char* strcpy(char* destination, const char* source);
// the size of the destination string should be large enough to store the copied string. Otherwise, it may result in undefined behavior.
// if 
/*
Buffer overflow. 
Finally strcpy(hostname, hp_h_name) and length(hostname, x), length(hp_h_name, b), a <  b. 
Buffer overflow :- !check(strlen(des), strlen(src), l1), strcpy(des, src), trans(l1, l2). 
*/
