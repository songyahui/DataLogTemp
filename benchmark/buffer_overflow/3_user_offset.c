// Online C compiler to run C program online
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* indexing (int index) {
    char *items[] = {"boat", "car", "truck", "train"};
    return items[index];
}

int main() {
    char* res = indexing (5);
    printf("\nYou selected %s\n", res);
}

/*
char *items[] = {"boat", "car", "truck", "train"};    =>    malloc(items, 4); 
Buffer_Overflow (loc) :- Finally exist access (items, bound) and bound > size 
*/
