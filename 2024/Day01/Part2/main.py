f = open("../input.txt")
dictionary = dict()
for str in f.read().splitlines():
    split = str.split()
    x = int(split[0])
    if x in dictionary:
        dictionary[x] = (dictionary[x][0] + 1, dictionary[x][1])
    else:
        dictionary[x] = (1,0)
    y = int(split[1])
    if y in dictionary:
        dictionary[y] = (dictionary[y][0] , dictionary[y][1] + 1)
    else:
        dictionary[y] = (0,1)
result = sum(map(lambda a: a[0]*a[1][0]*a[1][1],dictionary.items()))
print(result)