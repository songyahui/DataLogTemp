// Online C compiler to run C program online
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 10

char * copy_input(char *user_supplied_string){
    int i, dst_index;
    char *dst_buf = (char*)malloc(4*sizeof(char) * MAX_SIZE);
    printf("\nAllocated Length of dst_buf is  %lu", 4*sizeof(char) * MAX_SIZE);
    if ( MAX_SIZE <= strlen(user_supplied_string) ){
        return "0";
    }
    dst_index = 0;
    for ( i = 0; i < strlen(user_supplied_string); i++ ){
        if( '&' == user_supplied_string[i] ){
            dst_buf[dst_index++] = '&';
            dst_buf[dst_index++] = 'a';
            dst_buf[dst_index++] = 'm';
            dst_buf[dst_index++] = 'p';
            dst_buf[dst_index++] = ';';
        }
        else if ('<' == user_supplied_string[i] ){
        /* encode to &lt; */ 
        }
        else dst_buf[dst_index++] = user_supplied_string[i];
    }
    return dst_buf;
}

int main() {
    char * buf = copy_input("&&&&&&&&&");
    printf("\nActual    Length of dst_buf is  %lu\n", strlen(buf));
}

/*
char *loc = malloc (size)    =>    malloc(loc, size); 
Buffer_Overflow (loc) :- Finally exist access (loc, bound) and bound > size
for(init, condition, op)     =>    
*/
