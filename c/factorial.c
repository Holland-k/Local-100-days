#include <stdio.h>


unsigned long int fact(int i){
    unsigned long int acc = 1;
    while(i > 0){
        printf("the acc is %lu\n", acc);
        acc = acc * i;
        i--;
    }
    return acc;
}

int count_zero(int i){
    int count = 0;
    while(i > 9 && ((i%10) == 0)){
        count++;
        i = i / 10;
    }
    return count;
}

int main(void){
    int input, zeros;
    unsigned long int fac;
    input = 0;
	
	while(input < 1){
        printf("Please enter a number greater than 0.\n");
    	scanf("%d", &input);
    }

    fac = fact(input);
    printf("The factorial of %d is %lu.\n", input, fac);
    
    zeros = count_zero(fac);
    printf("The number of trailing zeros on %lu is %d.\n", fac, zeros);

    return 0;
}