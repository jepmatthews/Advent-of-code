class Cell:
    def __init__(self, coord, letter):
        self.coord = coord
        self.letter = letter
        self.region = None
        self.fences = 0
    def propagate_region(self,map,region):
        if(self.region == None):
            region.add_cell(self)
            for cell in map.get_neighbours(self.coord):
                if(cell == None or cell.letter != self.letter):
                    self.fences += 1
                else:
                    cell.propagate_region(map,region)

class Region:
    def __init__(self, letter):
        self.letter = letter
        self.cells = set()
    def add_cell(self,cell):
        self.cells.add(cell)
        cell.region = self
    def get_area(self):
        return len(self.cells)
    def get_perimeter(self):
        return sum(map(lambda cell: cell.fences,self.cells))
    def get_cost(self):
        return self.get_area() * self.get_perimeter()

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
    def get_neighbours(self):
        for (dx, dy) in [(1,0),(-1,0),(0,1),(0,-1)]:
            yield Coordinate(self.x+dx,self.y+dy)
    
class Map:
    def __init__(self, letterMap):
        self.map = letterMap
    def get_cell(self,coordinate):
        if(self.cell_in_map(coordinate.x,coordinate.y)):
            return self.map[coordinate.y][coordinate.x]
        else:
            return None
    def cell_in_map(self,x,y):
        if(0 <= x < len(self.map[0]) and 0 <= y < len(self.map)):
            return True
        else:
            return False
    def get_all_cells(self):
        for line in self.map:
            for cell in line:
                yield cell
    def get_neighbours(self, coordinate):
        for coord in coordinate.get_neighbours():
            yield self.get_cell(coord)
    


f = open("../input.txt")

rawMap = []
for (y,line) in enumerate(reversed(f.readlines())):
    cellLine = []
    for (x,char) in enumerate(line.replace("\n","")):
        cellLine.append(Cell(Coordinate(x,y),char))
    rawMap.append(cellLine)

cellMap = Map(rawMap)
regions = []

for cell in cellMap.get_all_cells():
    if(cell.region == None):
        newRegion = Region(cell.letter)
        cell.propagate_region(cellMap,newRegion)
        regions.append(newRegion)

result = 0
for region in regions:
    print(f'Region letter: {region.letter}, area: {region.get_area()}, perimeter: {region.get_perimeter()}, cost: {region.get_cost()}')
    result += region.get_cost()

print(result)
