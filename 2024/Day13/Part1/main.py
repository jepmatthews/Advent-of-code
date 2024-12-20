import re

class Vector:
    def __init__(self,dx,dy):
        self.dx = dx
        self.dy = dy
    def __add__(self,other):
        return Vector(self.dx + other.dx,self.dy, + other.dy)
    def __mul__(self,other):
        if type(other) is int:
            return Vector(self.dx * other, self.dy * other)
        else:
            return NotImplemented
    def __sub__(self,other):
        return Vector(self.dx - other.dx,self.dy - other.dy)
    def __floordiv__(self,other):
        if(not self.is_integer_multiple_of(other)):
            raise Exception("Doesn't divide exactly")
        return self.dy // other.dy
    def is_integer_multiple_of(self,other):
        return self.dx % other.dx == 0 and self.dy % other.dy == 0 and self.dx // other.dx == self.dy // other.dy


class Machine:
    def __init__(self,buttonA,buttonB,prize):
        self.buttonA = buttonA
        self.buttonB = buttonB
        self.prize = prize

def find_optimal_cost(machine):
    buttonAPresses=0
    while(buttonAPresses * machine.buttonA.dy < machine.prize.dy):
        vectorA = machine.buttonA * buttonAPresses
        if(machine.prize - vectorA).is_integer_multiple_of(machine.buttonB):
            return (buttonAPresses * 3) + ((machine.prize-vectorA) // machine.buttonB)
        buttonAPresses += 1
    return None

f = open("../input.txt")
regexMatchString = r"A: X\+(\d+).*Y\+(\d+)\n.+B: X\+(\d+).*Y\+(\d+)\nPrize: X=(\d+).+Y=(\d+)"

matches = re.findall(regexMatchString,f.read())

machines = []

for match in matches:
    machines.append(Machine(Vector(int(match[0]),int(match[1])),Vector(int(match[2]),int(match[3])),Vector(int(match[4]),int(match[5]))))

result = sum(filter(lambda cost: cost != None,map(lambda m: find_optimal_cost(m),machines)))

print(result)