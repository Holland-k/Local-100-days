import random

game = 1
board = ['']
diffc = -1

def create_board(sizei):
	global board
	board.pop()
	for i in range(0,sizei):
		t = ['*']
		for j in range(1,sizei):
			t.append('*')
		board.append(t)

	for i in range(0,(sizei*sizei)/2):
		c.append(randrange(0,9))

	for i in range(0,sizei)
		for j in range(0,sizei):
			n = randrange(0,9)


def start_game():
	global diffc
	print("Please select your difficulty level:\n\n")
	print("(E)asy (4x4)\n")
	print("(M)edium (10x10)\n")
	print("(H)ard (50x50)\n")

	d = None
	while not d:
		try: 
			d = str(raw_input())
			print("d = " + str(d))
			if(d == 'E' or d == 'e' or d == "Easy" or d == "easy"):
				#print("checking e diffc = " + str(diffc))
				diffc = 0
				#print("diff c = " + str(diffc))
				create_board(4)
			elif(d == 'M' or d == 'm' or d == "Medium" or d == "medium"):
				diffc = 1
				create_board(10)
			elif(d == 'H' or d == 'h' or d == "Hard" or d == "Hard"):
				#diffc = 2
				#create_board(50)
				print("not implement yet, please try another difficulty")
				raise ValueError
			else:
				raise ValueError
		except ValueError:
			print("Please enter easy, medium, or hard\n")

def print_board():
	print("diff = " + str(diffc))
	if diffc == 0:
		n = 4
	elif diffc == 1:
		n = 10
	else:
		n = 50
	line = "  "
	for i in range(0,n):
		print("n = " + str(n) + " i = " + str(i))
		line = line + (chr(ord('A')+i) + " ")
	print line
	for i in range(0,n):
		line = str(i+1) + " "
		for j in range(0,n):
			line = line + board[i][j] + " "
		print line

start_game()
while(game):
	print_board()
	print("Choose two cards (a1 b1)")

	try:
		n = str(raw_input())
		c = n.count(' ')
		if(c < 0 or c > 1):
			raise ValueError
		i = n.index(' ')
		in1 = n[:i]
		in2 = n[i+1:]
		print(in1)
		print(in2)
	except ValueError:
		print("Please choose two cards such as: a1 b2.\n")