import re

class Robot:
    def __init__(self,startX,startY,velX,velY):
        self.posX = startX
        self.posY = startY
        self.velX = velX
        self.velY = velY
    def move_steps(self,steps,xSize,ySize):
        self.posX = (self.posX + (steps * self.velX)) % xSize
        self.posY = (self.posY + (steps * self.velY)) % ySize


xSize = 101
ySize = 103
numberOfSeconds = 100
f = open("../input.txt")
regexMatchString = r"p=(-*\d+),(-*\d+) v=(-*\d+),(-*\d+)"

matches = re.findall(regexMatchString,f.read())
robots = []
for match in matches:
    robots.append(Robot(int(match[0]),int(match[1]),int(match[2]),int(match[3])))

sectors = [[],[],[],[]]

for robot in robots:
    robot.move_steps(numberOfSeconds,xSize,ySize)
    if(robot.posX < xSize//2 and robot.posY < ySize // 2):
        sectors[0].append(robot)
    if(robot.posX > xSize//2 and robot.posY < ySize // 2):
        sectors[1].append(robot)
    if(robot.posX < xSize//2 and robot.posY > ySize // 2):
        sectors[2].append(robot)
    if(robot.posX > xSize//2 and robot.posY > ySize // 2):
        sectors[3].append(robot)

result = 1
for sector in sectors:
    result *= len(sector)

print(result)

