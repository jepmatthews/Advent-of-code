class Coordinate:
    def __init__(self,x,y):
        self.x = x
        self.y = y
    def __str__(self):
        return f"(x: {self.x},y: {self.y})"
    def __eq__ (self,other):
        if not isinstance(other, Coordinate):
            return NotImplemented
        return self.x == other.x and self.y == other.y
    def __hash__(self):
        return hash((self.x,self.y))
    def vectorToCoord(self,other):
        return Vector(other.x-self.x,other.y-self.y)
    def addVector(self,vector):
        return Coordinate(self.x + vector.delX,self.y + vector.delY)
    def subVector(self,vector):
        return Coordinate(self.x - vector.delX,self.y - vector.delY)

class Vector:
    def __init__(self,delX,delY):
        self.delX = delX
        self.delY = delY

f = open("../input.txt")

lines = list(map(lambda string: string.replace("\n",""),reversed(f.readlines())))

maxX = len(lines[0]) - 1
maxY = len(lines) - 1

def coordinateInMap(coord):
    return 0 <= coord.x <= maxX and 0 <= coord.y <= maxY 

antennaDictionary = {}

for y,line in enumerate(lines):
    for x,char in enumerate(line):
        if(char != "."):
            if(char not in antennaDictionary):
                antennaDictionary[char] = []
            antennaDictionary[char].append(Coordinate(x,y))

antinodes = set()

for _,antennas in antennaDictionary.items():
    for index,antennaCoordinateA in enumerate(antennas[:-1]):
        for antennaCoordinateB in antennas[index+1:]:
            vectorAToB = antennaCoordinateA.vectorToCoord(antennaCoordinateB)
            antiNodeB = antennaCoordinateB.addVector(vectorAToB)
            antiNodeA = antennaCoordinateA.subVector(vectorAToB)
            antinodes.add(antennaCoordinateA)
            antinodes.add(antennaCoordinateB)
            while(coordinateInMap(antiNodeA)):
                antinodes.add(antiNodeA)
                antiNodeA = antiNodeA.subVector(vectorAToB)                
            while coordinateInMap(antiNodeB):                
                antinodes.add(antiNodeB)
                antiNodeB = antiNodeB.addVector(vectorAToB)

print(list(map(str,antinodes)))
print(len(antinodes))
