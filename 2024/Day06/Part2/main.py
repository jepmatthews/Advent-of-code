from enum import Enum

class Direction(Enum):
    North = 'N'
    East = 'E'
    South = 'S'
    West = 'W'

class Cell(Enum):
    Empty = '.'
    Visited = 'X'
    Obstacle = '#'

class Coordinate:
    def __init__(self,x,y):
        self.x = x
        self.y = y
    def __str__(self):
        return f"(x: {self.x},y: {self.y})"
    def moveDirection(self,direction):
        if direction is Direction.North:
            return Coordinate(self.x,self.y+1)
        elif direction is Direction.East:
            return Coordinate(self.x+1,self.y)
        elif direction is Direction.South:
            return Coordinate(self.x,self.y-1)
        elif direction is Direction.West:
            return Coordinate(self.x-1,self.y)
        
class Map:
    def __init__(self,map,startPosition,startDirection):
        self.map = map
        self.position = startPosition
        self.maxX = len(map[0]) - 1
        self.maxY = len(map) - 1
        self.direction = startDirection
    def guardIsInMap(self):
        return 0 <= self.position.x <= self.maxX and 0 <= self.position.y <= self.maxY
    def takeStep(self):
        nextPosition = self.position.moveDirection(self.direction)
        if  (0 <= nextPosition.x <= self.maxX and 0 <= nextPosition.y <= self.maxY) \
                and (self.map[nextPosition.y][nextPosition.x] == Cell.Obstacle):
            self.direction = turn_right(self.direction)
        else:
            self.map[self.position.y][self.position.x] = Cell.Visited
            self.position = nextPosition
    def __str__(self):
        return "\n".join(map(lambda line: "".join(map(lambda cell: cell.value,line)),reversed(self.map)))

def turn_right(direction):
    if direction is Direction.North:
        return Direction.East
    elif direction is Direction.East:
        return Direction.South
    elif direction is Direction.South:
        return Direction.West
    elif direction is Direction.West:
        return Direction.North


f = open("../input.txt")

lines = f.readlines()

lines = list(reversed(lines))
# print(list(enumerate(lines)))
for y, line in enumerate(lines):
    for x,ch in enumerate(line):
        if(ch == "^"):
            startCoordinate = Coordinate(x,y)

lines = list(map(lambda string: string.replace("\n","").replace("^",""),lines))
            
lines = list(map(lambda line: list(map(Cell,line)),lines))

labMap = Map(lines,startCoordinate,Direction.North)

print(labMap)
print("\n")
while(labMap.guardIsInMap()):
    labMap.takeStep()

print(sum(map(lambda line: line.count(Cell.Visited),labMap.map)))