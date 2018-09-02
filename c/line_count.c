#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]){
	FILE *fp;
	char ch;
	int lc = 0;

	if(argc != 2){
		printf("Input should be in the format of \"line_count <filename>\".\n");
		return 1;
	}
	fp = fopen(argv[1], "r");
	if(fp == NULL){
		printf("Cannot open file: %s\n", argv[1]);
		return 1;
	}
	ch = fgetc(fp);
	while(ch != EOF){
		if(ch == '\n'){
			lc++;
		}
		ch = fgetc(fp);
	}
	fclose(fp);
	printf("The file %s contains %d lines.\n", argv[1], lc);
	return 0;
}