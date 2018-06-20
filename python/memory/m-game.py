game = 1

def print_board():
	pass

while(game):
	print_board()
	print("Choose two cards (a1 b1)")
	
	try:
		n = str(raw_input())
		if(i = n.index(' ')):
			in1 = n[:i]
			in2 = n[i+1:]
			print(in1)
			print(in2)
	except ValueError:
		print("Please enter a number.\n")