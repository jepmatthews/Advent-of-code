f = open("../input.txt")
list1 = []
list2 = []
for str in f.read().splitlines():
    split = str.split()
    list1.append(int(split[0]))
    list2.append(int(split[1]))
list1.sort()
list2.sort()
result = sum(map(lambda a,b: abs(a - b),list1,list2))
print(result)