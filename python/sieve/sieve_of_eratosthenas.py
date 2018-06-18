
def print_primes(a):
	for i in range(0,len(a)-1):
		if(a[i] == True):
			print(str(i+1)+" ")
	print("\n")

def init_array(n):
	a = []
	for i in range(0,n):
		a.append(True)
	return a

def soe(n):
	a = init_array(n)

	for i in range(2,n):
		#print("i = " + str(i))
		if(a[i-1] == True):
			j = (i + i)-1
			#print("j = " + str(j))
			while(j < len(a)):
				a[j] = False
				j = j + i
	print_primes(a)

print("This program will return the first x prime numbers from the range of 1 to n. Please enter n:")
n = None
while not n:
	try:
		n = int(raw_input())
	except ValueError:
		print("Please enter a number.\n")

soe(n)
