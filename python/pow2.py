import itertools

x = []
y = str(10)
for i in y:
	x.append(int(i))

z = itertools.permutations(x)
for i in z:
	k = ""
	for j in i:
		k = k + str(j)
	if(len(str(int(k))) == len(y)):
		print(k)
		if((int(k) != 0) and ((int(k) & (int(k) - 1)) == 0)):
			print("True")
