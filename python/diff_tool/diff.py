import os, sys

### Very simple diff file, need to implement a look-ahead feature

def main(file1, file2):
	fd1 = open(file1, 'r')
	fd2 = open(file2, 'r')
	for i,j in zip(fd1.readlines(),fd2.readlines()):
		if(i != j):
			print(file1 + ": " + i + " <=> " + file2 + ": " + j)
	return 0


try:
	fn1 = sys.argv[1]
	fn2 = sys.argv[2]
except:
	print(str(sys.argv))
	print("Please enter two file names.")

main(fn1, fn2)
