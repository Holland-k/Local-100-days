import random

game = 1
board = ['']
diffc = -1

def check_for_win():
	for i in range(0, len(board)):
		for j in range(0, len(board)):
			if(board[i][j]["disc"] == False):
				return 0
	return 1

def rand_arr(cur_arr):
	i = len(cur_arr)-1
	#print("starting array " + str(cur_arr) + " and i " + str(i))
	while(i > 0):
		j = random.randrange(0,i)
		#print("j " + str(j) + " i " + str(i) + " cur array " + str(cur_arr))
		cur_arr[j],cur_arr[i] = cur_arr[i],cur_arr[j]	
		i -= 1
	#print("ending array " + str(cur_arr))
	return cur_arr

def create_board(sizei):
	global board
	board.pop()
	#initialize board
	for i in range(0,sizei):
		t = [{"face":'*',"val":-1,"disc":False}]
		for j in range(1,sizei):
			t.append({"face":'*',"val":-1,"disc":False})
		board.append(t)

	j = 0
	c = ['']
	c.pop()
	half = (sizei*sizei) / 2
	for i in range(0,half):
		c.append(j)
		j += 1
		if(j>9):
			j = 0

	#print("before rand " + str(c))
	c = rand_arr(c)
	print("after rand " + str(c))

	k = 0
	for i in range(0,sizei):
		for j in range(0,sizei):
			board[i][j]["val"] = c[k]
			k += 1
			if(k >= half):
				k = 0
				c = rand_arr(c)
				print("after next rand " + str(c))


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
			line = line + board[i][j]["face"] + " "
		print line

def print_board_cheater():
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
			line = line + str(board[i][j]["val"]) + " "
		print line

start_game()
while(game):
	print("gaming")
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
		if(in1 == "cheat"):
			print_board_cheater()
		print(in1)
		print(in2)
	except ValueError:
		print("Please choose two cards such as: a1 b2.\n")