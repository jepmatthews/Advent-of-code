from enum import Enum

class Direction(Enum):
    North = 'N'
    East = 'E'
    South = 'S'
    West = 'W'

class Fence:
    def __init__(self,start,end,side):
        self.start = start
        self.end = end
        self.side = side
    def add_to_end(self,fence):
        if(fence.side == self.side):
            self.end = fence.end
        else:
            raise Exception("Tried to add fence on different side")
    def add_to_start(self,fence):
        if(fence.side == self.side):
            self.start = fence.start
        else:
            raise Exception("Tried to add fence on different side")


class Cell:
    def __init__(self, coord, letter):
        self.coord = coord
        self.letter = letter
        self.region = None
    def get_fence(self,side):
        if(side == Direction.North):
            return Fence(Coordinate(self.coord.x,self.coord.y+1),Coordinate(self.coord.x+1,self.coord.y+1),side)
        if(side == Direction.East):
            return Fence(Coordinate(self.coord.x+1,self.coord.y),Coordinate(self.coord.x+1,self.coord.y+1),side)
        if(side == Direction.South):
            return Fence(Coordinate(self.coord.x,self.coord.y),Coordinate(self.coord.x+1,self.coord.y),side)
        if(side == Direction.West):
            return Fence(Coordinate(self.coord.x,self.coord.y),Coordinate(self.coord.x,self.coord.y+1),side)
    def propagate_region(self,map,region):
        if(self.region == None):
            region.add_cell(self)
            neighbours = list(map.get_neighbours(self.coord))
            for (direction,cell) in neighbours:
                if(cell == None or cell.letter != self.letter):
                    newFence = self.get_fence(direction)
                    i = 0
                    while i < len(region.fences):
                        fence = region.fences[i]
                        if(newFence.side == fence.side):
                            if(fence.start == newFence.end):
                                newFence.add_to_end(fence)
                                region.fences.remove(fence)
                                i=0
                            if(fence.end == newFence.start):
                                newFence.add_to_start(fence)
                                region.fences.remove(fence)
                                i=0
                        i += 1
                    region.fences.append(newFence)
            for (_,cell) in neighbours:
                if(cell != None and cell.letter == self.letter):
                    cell.propagate_region(map,region)


class Region:
    def __init__(self, letter):
        self.letter = letter
        self.cells = set()
        self.fences = []
    def add_cell(self,cell):
        self.cells.add(cell)
        cell.region = self
    def get_area(self):
        return len(self.cells)

    def get_cost(self):
        return self.get_area() * len(self.fences)

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
        for direction in [Direction.North,Direction.East,Direction.South,Direction.West]:
            yield (direction,self.get_coordinate_in_direction(direction))
    def get_coordinate_in_direction(self,direction):
        if(direction == Direction.North):
            return Coordinate(self.x,self.y+1)
        if(direction == Direction.East):
            return Coordinate(self.x+1,self.y)
        if(direction == Direction.South):
            return Coordinate(self.x,self.y-1)
        if(direction == Direction.West):
            return Coordinate(self.x-1,self.y)
    
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
        for (direction,coord) in coordinate.get_neighbours():
            yield (direction,self.get_cell(coord))
    


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

result = sum(map(lambda r: r.get_cost(),regions))

