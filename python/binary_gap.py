j = -1
d = 0
s = "{0:b}".format(6)
print(s)
for i in range(len(s)):
	if s[i] == "1":
		if j == -1:
			j = i
		elif((i - j) > d):
			print("old d " + str(d))
			d = i - j
			print("new d " + str(d) + " i " + str(i) + " j " + str(j))
			j = i
		else:
			j = i
print(d)