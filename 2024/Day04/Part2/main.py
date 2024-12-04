f = open("../input.txt")
result = 0
lines = f.readlines()

for y in range(1,len(lines) - 1):
    for x in range(1,len(lines[0]) - 1):
        if lines[y][x] == "A":
            neighbours = [lines[y+1][x+1],lines[y+1][x-1],lines[y-1][x+1],lines[y-1][x-1]]
            if(neighbours.count("M") == 2 and neighbours.count("S") == 2 and lines[y+1][x+1] != lines[y-1][x-1]):
                result += 1

print(result)