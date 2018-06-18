

def colcon(n):
	steps = 0

	while(n > 1):
		print("n = " + str(n))
		if(n % 2 == 0):
			n = n / 2
			steps += 1
		else:
			n = n * 3 + 1
			steps += 1
	print(steps)

print("This program will return the number of steps it takes the Collatz Conjecture\nto reach 1 from n. N must be greater than 1. Please enter n:")
n = None
while not n:
	try:
		n = int(raw_input())
	except ValueError:
		print("Please enter a number.\n")

colcon(n)