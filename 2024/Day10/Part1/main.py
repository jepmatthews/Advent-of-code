class HeightMap:
    def __init__(self,heightMap):
        self.heightMap = heightMap
    def __getitem__(self,key):
        return self.heightMap[key.y][key.x]
    def get_cells(self):
        for y, line in enumerate(self.heightMap):
            for x, char in enumerate(line):
                yield (Coordinate(x,y),char)
    def get_neighbours(self,coordinate):
        for dx,dy in [(1,0),(-1,0),(0,1),(0,-1)]:
            if(0 <= coordinate.x + dx < len(self.heightMap[0]) and 0 <= coordinate.y + dy < len(self.heightMap)):
                yield Coordinate(coordinate.x + dx, coordinate.y + dy)
    def get_neighbours_one_cell_up(self,coordinate):
        for neighbourCoord in self.get_neighbours(coordinate):
            if self[neighbourCoord] == self[coordinate] + 1:
                yield neighbourCoord

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
    

f = open("../input.txt")

heightMap = HeightMap(list(map(lambda string: list(map(int, string.replace("\n",""))),reversed(f.readlines()))))

for line in reversed(heightMap.heightMap):
    print(line)

totalTrails = 0

for startCoord,cell in (heightMap.get_cells()):
    if(cell == 0):
        
        locationStack = [(startCoord,list(heightMap.get_neighbours_one_cell_up(startCoord)))]
        trailheadEnds = set()
        while(len(locationStack) > 0):
            if len(locationStack[-1][1]) == 0:
                locationStack.pop()
            elif heightMap[locationStack[-1][0]] == 8:
                for endCoord in locationStack[-1][1]:
                    trailheadEnds.add(endCoord)
                locationStack.pop()
            else:
                newLocation = locationStack[-1][1].pop()
                newNeighboursOneCellUp = list(heightMap.get_neighbours_one_cell_up(newLocation))
                locationStack.append((newLocation,newNeighboursOneCellUp))
        print (len(trailheadEnds))
        totalTrails += len(trailheadEnds)

print(totalTrails)