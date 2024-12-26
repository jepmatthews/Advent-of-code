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

i = 0
minScore = 47833201
while(i<10000000000000):
    robotMap = []
    sectors = [0,0,0,0]
    j = 0
    while(j < ySize):
        robotMap.append([0] * xSize)
        j += 1
    for robot in robots:
        robot.move_steps(1,xSize,ySize)
        robotMap[robot.posY][robot.posX] = robotMap[robot.posY][robot.posX] + 1

    maxRobotsInALine = 0
    for line in robotMap:
        prevN = 0
        robotsInLine = 0
        for n in line:
            if(n != 0):
                robotsInLine += 1
                if(maxRobotsInALine < robotsInLine):
                    maxRobotsInALine = robotsInLine
            else:
                robotsInLine = 0
            prevN = n
    
    if maxRobotsInALine > 6:
        print (i + 1)
        print(maxRobotsInALine)
        for line in robotMap:
            for num in line:
                if num != 0:
                    print('\033[32m',end="")
                    print(num,end="")
                    print('\033[0m',end="")
                else:
                    print(num,end="")
            print("")
        if(input("Is it a tree?") == "yes"):
            break    
    i += 1

