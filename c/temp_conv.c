#include <stdio.h>

#define LOWER_LIMIT 0
#define UPPER_LIMIT 1000

int main(void){

	int limit_low = -1;
	int limit_high = 1001;
	int step = 2000;

	printf("This program will take three numbers, starting temp, ending temp, and a step size.\n");
	while(limit_low < LOWER_LIMIT){
		printf("Please enter your starting temp, the temp must be >= %d: ", (int) LOWER_LIMIT);
    	scanf("%d", &limit_low);
    }
    while((limit_high > UPPER_LIMIT) || (limit_high < limit_low)){
    	printf("Please enter your ending temp, the temp must be <= %d: ", (int) UPPER_LIMIT);
    	scanf("%d", &limit_high);
    }
    while(step > (limit_high - limit_low)){
    	printf("Please enter the step size, the step cannot be larger than the delta of the previous two numbers\n");
    	scanf("%d", &step);
    }

    printf("Celsius\t\tFarenheit\n");
    printf("-------\t\t---------\n");
    while(limit_low < limit_high){
    	printf("%d\t\t%d\n", limit_low, (limit_low*9) / 5 +32);
    	limit_low = limit_low + step;
    }
    return 0;
}