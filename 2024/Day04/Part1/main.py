class DiagonalIterator:
    def __init__(self, array,startX,startY,direction):
        self.array = array
        self.x = startX
        self.y = startY
        self.direction = direction
    def __iter__(self):
        return self
    def __next__(self):
        if(self.y >= len(self.array) or self.x >= len(self.array[0]) or self.y < 0 or self.x < 0):
            raise StopIteration
        else:
            result = self.array[self.y][self.x]
            if self.direction == "NE":
                self.x += 1
                self.y -= 1
            elif self.direction == "SE":
                self.x += 1
                self.y += 1
            elif self.direction == "SW":
                self.x -= 1
                self.y += 1
            return result

def countXmas(string):
    return string.count("XMAS") + string.count("SAMX")

f = open("../input.txt")
count = 0
lines = f.readlines()
lines = list(map(lambda str: str.replace("\n",""), lines))
for line in lines:
    count += countXmas(line)

x = 0
y = 0
while(True):
    if x >= len(lines[0]):
        break
    count += countXmas(''.join(iter(DiagonalIterator(lines,x,y,'NE'))))
    if y < len(lines) - 1:
        y += 1
    else:
        x += 1

x = 0
y = len(lines) - 1
while(True):
    if x >= len(lines[0]):
        break
    count += countXmas(''.join(iter(DiagonalIterator(lines,x,y,'SE'))))
    if y > 0:
        y -= 1
    else:
        x += 1

lines = list(map(list,zip(*lines)))

for line in lines:
    count += countXmas(''.join(line))

print(count)