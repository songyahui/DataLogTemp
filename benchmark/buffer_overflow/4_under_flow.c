// Online C compiler to run C program online
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int getValueFromArray(int *array, int len, int index) {
    // error is index is negative. 

    int value;

// check that the array index is less than the maximum 
// length of the array 
    if (index < len) {
// get the value at the specified index of the array 
        value = array[index];
    }
// if array index is invalid then output error message 
// and return value indicating error 
    else {
        printf("Value is: %d\n", array[index]);
        value = -1;
    }

    return value;
}

int main() {
    int arr[5] = {1, 2, 3, 4, 5}; 
    int res = getValueFromArray(arr, 3, -2);
    printf ("%i\n", res);
}

/*
int arr[5] = {1, 2, 3, 4, 5};     =>    malloc(arr, 5); 
Buffer_Overflow (arr) :- Finally exist access (arr, bound) and bound > size 
*/
