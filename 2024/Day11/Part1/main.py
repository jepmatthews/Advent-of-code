def next_line(line):
    newLine = []
    for stone in line:
        if(stone == 0):
            newLine.append(1)
            continue
        stoneString = str(stone) 
        if(len(stoneString) %2 == 0):
            newLine.append(int(stoneString[0:len(stoneString)//2]))
            newLine.append(int(stoneString[len(stoneString)//2:]))
        else:
            value = (stone * 2024)
            newLine.append(value)
    return newLine


f = open("../input.txt")
stones = map(lambda i: int(i),f.readline().split())
i=1
while(i <= 25):
    stones = next_line(stones)
    i +=1

print(len(stones))


