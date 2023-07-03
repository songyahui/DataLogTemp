#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <stddef.h>
 
#define AF_INET 1024
typedef char in_addr_t;



void validate_addr_form(char *user_supplied_addr){
    return ;
}

/*
typedef int in_addr;
unsigned long inet_addr(
  const char *cp
);
*/
in_addr_t inet_addr(const char *cp);

typedef struct hostent {
  char  *h_name;
  char  **h_aliases;
  short h_addrtype;
  short h_length;
  char  **h_addr_list;
} HOSTENT, *PHOSTENT, *LPHOSTENT;


struct hostent *gethostbyaddr(char *host_address,
                              int address_length,
                              int address_type);


void host_lookup(char *user_supplied_addr){
    struct hostent *hp;
    in_addr_t *addr;
    char hostname[64];

/*routine that ensures user_supplied_addr is in the right format for conversion */ 

    validate_addr_form(user_supplied_addr);
    *addr = inet_addr(user_supplied_addr);
    hp = gethostbyaddr(addr, 4, AF_INET);
    strcpy(hostname, hp->h_name);
}

// Driver Code
int main()
{
    return 0;
}
