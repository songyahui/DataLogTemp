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
/*
{
    struct hostent *hp;
    *hp->h_name = "aaaaaaaaaaaaaaaa";
    return hp;
}
*/

void host_lookup(char *user_supplied_addr){
    struct hostent *hp;
    in_addr_t *addr;
    char hostname[64];

/*routine that ensures user_supplied_addr is in the right format for conversion */ 

    validate_addr_form(user_supplied_addr);
    *addr = inet_addr(user_supplied_addr);
    hp = gethostbyaddr(addr, 4, AF_INET);
    strcpy(hostname, hp->h_name);
    printf ("%s", hostname);
}

int main()
{
    char * address = "address"; 
    host_lookup (address);
    return 0;
}

// char* strcpy(char* destination, const char* source);
// the size of the destination string should be large enough to store the copied string. Otherwise, it may result in undefined behavior.

/*
Buffer overflow. 
Finally strcpy(hostname, hp_h_name) and length(hostname, a), length(hp_h_name, b), a <  b. 
*/
